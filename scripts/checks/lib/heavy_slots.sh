#!/usr/bin/env bash
# One host-wide pair of persistent inodes, shared by all runner users and clones.
# Provisioning is documented in scheduled/README.md. Never create a private pool.
heavy_slot_acquire() {
  local pool="${CONCRETE_HEAVY_SLOT_DIR:-/var/lib/concrete/heavy-slots}" slot
  if [ ! -d "$pool" ] || [ -L "$pool" ] || [ ! -f "$pool/slot-1" ] || [ ! -f "$pool/slot-2" ] ||
     [ -L "$pool/slot-1" ] || [ -L "$pool/slot-2" ]; then
    echo 'INCOMPLETE heavy slots: host pool is not provisioned' >&2; return 1
  fi
  [ -z "${CONCRETE_HEAVY_SLOT_FD:-}" ] || {
    echo 'INCOMPLETE heavy slots: nested acquisition forbidden' >&2; return 1;
  }
  for slot in 1 2; do
    if ! exec {CONCRETE_HEAVY_SLOT_FD}<>"$pool/slot-$slot"; then
      echo 'INCOMPLETE heavy slots: host pool is not accessible' >&2; return 1
    fi
    if flock -n "$CONCRETE_HEAVY_SLOT_FD"; then return 0; fi
    exec {CONCRETE_HEAVY_SLOT_FD}>&-
    unset CONCRETE_HEAVY_SLOT_FD
  done
  echo 'INCOMPLETE heavy slots: both host slots occupied' >&2
  return 1
}
heavy_slot_release() {
  if [ -n "${CONCRETE_HEAVY_SLOT_FD:-}" ]; then
    exec {CONCRETE_HEAVY_SLOT_FD}>&-
    unset CONCRETE_HEAVY_SLOT_FD
  fi
}
