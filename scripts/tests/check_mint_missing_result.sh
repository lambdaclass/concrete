#!/usr/bin/env bash
# ONE ACCOUNTING REFUSAL, CHEAPLY ENOUGH TO MUTATE.
#
# check_mint_batch_accounting.sh runs the dependency-edge gate once per self-test flag — one run per
# case, on the order of an hour. As a mutation family's declared gate that is four legs, over three hours for
# a single family, which is not a price a campaign should pay to cover one refusal.
#
# This runs the ONE case that covers the missing-result refusal: a probe that never reported must be
# named as missing, not silently counted as a pass. The comprehensive gate still runs the full set in
# CI; this exists so the refusal can be mutation-covered without dominating the campaign.
set -uEo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
exec env MBA_ONLY=MINT_SELFTEST_BREAK bash "$ROOT_DIR/scripts/tests/check_mint_batch_accounting.sh"
