#!/usr/bin/env bash
# One host-wide pair of persistent inodes, shared by all runner users and clones.
# Provisioning is documented in scheduled/README.md. Never create a private pool.
heavy_slot_acquire() {
  local pool="${CONCRETE_HEAVY_SLOT_DIR:-/var/lib/concrete/heavy-slots}" slot
  if [ ! -d "$pool" ] || [ -L "$pool" ] || [ ! -f "$pool/slot-1" ] || [ ! -f "$pool/slot-2" ] ||
     [ -L "$pool/slot-1" ] || [ -L "$pool/slot-2" ]; then
    echo 'INCOMPLETE heavy slots: host pool is not provisioned' >&2; return 1
  fi
  if [ -n "${CONCRETE_HEAVY_SLOT_FD:-}" ]; then
    # Children inherit the same open file description. Closing their copy never
    # unlocks the parent's copy; do not use flock -u on a shared description.
    if [[ "$CONCRETE_HEAVY_SLOT_FD" =~ ^[0-9]+$ ]] &&
       { [ "/dev/fd/$CONCRETE_HEAVY_SLOT_FD" -ef "$pool/slot-1" ] ||
         [ "/dev/fd/$CONCRETE_HEAVY_SLOT_FD" -ef "$pool/slot-2" ]; } &&
       flock -n "$CONCRETE_HEAVY_SLOT_FD"; then
      export CONCRETE_HEAVY_SLOT_FD
      return 0
    fi
    echo 'INCOMPLETE heavy slots: invalid inherited slot descriptor' >&2
    return 1
  fi
  for slot in 1 2; do
    if ! exec {CONCRETE_HEAVY_SLOT_FD}<>"$pool/slot-$slot"; then
      echo 'INCOMPLETE heavy slots: host pool is not accessible' >&2; return 1
    fi
    if flock -n "$CONCRETE_HEAVY_SLOT_FD"; then export CONCRETE_HEAVY_SLOT_FD; return 0; fi
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
