#!/usr/bin/env bash
# emits "<unit> finished" for each concrete-* user unit that goes inactive; runs until none left active
declare -A seen
while true; do
  act=$(systemctl --user list-units --type=service --state=active --no-legend 'concrete-*' | awk '{print $1}' | sed 's/\.service$//')
  for u in $act; do seen[$u]=1; done
  for u in "${!seen[@]}"; do
    if [ "${seen[$u]}" = 1 ] && ! grep -qx "$u" <<<"$act"; then echo "$u finished"; seen[$u]=2; fi
  done
  sleep 20
done
