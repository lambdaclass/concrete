#!/usr/bin/env bash
# Select the scheduled single-attempt policy; bounded.sh owns retry and status.
source "$(dirname "${BASH_SOURCE[0]}")/bounded.sh"
BOUNDED_EXEC="$(dirname "$BOUNDED_SH")/scheduled_process.py"
BOUNDED_SCHEDULED=1
