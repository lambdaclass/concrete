#!/usr/bin/env bash
# NORMALISE A PUBLISHED CAMPAIGN ARTIFACT FOR COMPARISON AGAINST A GOLDEN BASELINE.
#
# The artifact is the campaign's authoritative record, and a golden baseline is how a schema change
# becomes visible instead of silent. But a few of its values cannot be equal across two runs of the
# same command: elapsed seconds, a run id built from a timestamp and a pid, the evidence directory
# derived from that id, and an evidence root that digests transcripts containing timestamps and
# temporary paths.
#
# So exactly those values are replaced by <VOLATILE>, and NOTHING ELSE IS TOUCHED. Every count, every
# disposition, every digest of stable content, the key set and its order are all compared byte for
# byte.
#
# THE VOLATILE LIST IS PART OF THE CONTRACT. Widening it is the obvious way to make a failing
# comparison pass, so it lives here in one place, is short enough to read, and changes to it show up
# in review as changes to this file rather than as a baseline that quietly started matching again.
# A key is on this list only because two runs of the SAME producer against the SAME commit cannot
# agree on it — not because it is inconvenient.
set -uo pipefail

# THE VOLATILE SET, BY EXACT NAME, AND THE ONE PLACE IT IS DEFINED.
#
# A key is here only because two runs of the SAME producer, against a repository that has legitimately
# moved forward, cannot agree on it. Each is named with its reason, because "eight fields" is not a
# contract and a set that can be widened silently is not a pin.
#
#   timings          two runs never take the same number of seconds
#   run_id           timestamp + pid + entropy, unique by construction
#   evidence_dir     derived from run_id
#   evidence_root    digests transcripts containing timestamps and temp paths
#   head             THE COMMIT UNDER TEST. Pinning it makes the baseline stale the instant anything
#                    is committed — including committing the baseline itself, which is circular.
#   workspace_head   the same commit, observed in the disposable workspace
#   baseline_compiler_sha
#                    the compiler binary built during the run. MEASURED, not assumed: the binary
#                    embeds its own absolute build path, and the campaign builds in a fresh
#                    /tmp/concrete-mut.<pid>.<rand> workspace, so two runs at the SAME commit
#                    produce different bytes. Comparing it would make this gate flaky rather than
#                    strict, which is the one failure mode a freshness gate must not have.
#
# DELIBERATELY NOT VOLATILE, though an earlier version of this list masked them:
#
#   *_driver_sha, inventory_sha, families_digest
#                    These are digests of FILE CONTENT and are stable across runs at one commit.
#                    Masking them was over-correction: it removed exactly the signal that makes this
#                    a FRESHNESS gate. A changed producer, or a one-for-one family substitution that
#                    leaves every count identical, must make the baseline stale — that is the whole
#                    point of committing one. The cost is that a commit touching the driver requires
#                    a deliberate regeneration, which is the intended price.
#
# What REMAINS compared is therefore the whole semantic record — every count, every disposition,
# mode, integrity, qualification, the gate-proven split — plus the producer's own identity and the
# tree-state digests, which for a clean tree are the constant empty-diff digest.
VOLATILE_KEYS="secs_total secs_copy secs_build secs_gate secs_other run_id evidence_dir evidence_root head workspace_head baseline_compiler_sha"

usage() { echo "usage: normalize_campaign_summary.sh <artifact-path>" >&2; exit 2; }
[ $# -eq 1 ] || usage
[ -s "$1" ] || { echo "error: '$1' is missing or empty" >&2; exit 2; }

# A LINE THAT IS NOT AN ASSIGNMENT IS NOT NORMALISED AWAY. It is passed through unchanged, so a
# malformed artifact fails the comparison instead of being silently tidied into one that matches.
awk -v vol="$VOLATILE_KEYS" '
  BEGIN { n = split(vol, a, " "); for (i = 1; i <= n; i++) isvol[a[i]] = 1 }
  {
    if (match($0, /^[a-z_]+=/)) {
      key = substr($0, 1, RLENGTH - 1)
      if (key in isvol) { print key "=<VOLATILE>"; next }
    }
    print
  }
' "$1"
