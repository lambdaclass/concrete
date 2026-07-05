#!/usr/bin/env bash
# End-of-Phase-2 VC/SMT examples gate (ROADMAP Phase 2 #5-9).
#
# Each example forces ONE named VC/SMT surface and carries a report gate. Together
# they show: kernel-owned facts (linear, division/modulo summaries, path
# conditions) stay on omega; the one genuine external-solver surface is the signed
# nonlinear product; and every "bad" variant stays a non-proof.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
D="examples/vc_suite"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# status of a specific obligation's first status line
st(){ awk -v a="$2" 'index($0,a){f=1} f&&/status:/{print; exit}' <<<"$("$COMPILER" "$1" --report vcs 2>/dev/null)"; }
proved(){ grep -qF "proved_by_kernel_decision (omega)" <<<"$(st "$1" "$2")" && ok "$3" || no "$3 (got: $(st "$1" "$2" | sed 's/^ *//'))"; }
unproven(){ grep -qF "unproven" <<<"$(st "$1" "$2")" && ok "$3" || no "$3 (expected unproven)"; }
no_query(){ grep -qiF "no SMT-eligible" <<<"$("$COMPILER" "$1" --report vcs --emit-smt 2>/dev/null)" && ok "$2" || no "$2 (unexpected SMT query)"; }

echo "=== #5 packet_window: mixed bounds; bad length stays unproven ==="
proved   "$D/packet_window.con" "payload_end#aa0"   "payload_end → omega (bounds line up)"
unproven "$D/packet_window.con" "unchecked_end#aa0" "unchecked_end → unproven (bad length)"

echo "=== #6 fixed_point_filter: linear=omega, signed product=SMT ==="
awk '/accumulate#ovf0/{f=1} f&&/status:/{print;exit}' <<<"$("$COMPILER" "$D/fixed_point_filter.con" --report vcs 2>/dev/null)" | grep -qF "proved_by_kernel_decision (omega)" \
  && ok "accumulate (linear) → omega, not SMT" || no "accumulate not omega-proved"
grep -qF "scale#ovf0" <<<"$("$COMPILER" "$D/fixed_point_filter.con" --report vcs --emit-smt 2>/dev/null)" \
  && ok "scale (signed nonlinear product) → SMT-eligible (query emitted)" || no "scale not SMT-eligible"

echo "=== #7 chunked_hash_padding: block-count division summary = omega (#21) ==="
proved  "$D/chunked_hash_padding.con" "nblocks#aa0" "nblocks (len+72)/64 <= 6 → omega"
no_query "$D/chunked_hash_padding.con" "block-count is kernel-owned → no SMT query"

echo "=== #8 rate_limiter: path feasibility = omega (#22); weakened guard unproven ==="
proved   "$D/rate_limiter.con" "clamp_count#aa0" "clamp_count → omega (path conditions)"
unproven "$D/rate_limiter.con" "weak_clamp#aa0"  "weak_clamp → unproven (weakened guard)"

echo "=== #9 ring_buffer_indices: wraparound modulo = omega (#21) ==="
proved  "$D/ring_buffer_indices.con" "wrap#aa0" "i % 16 ∈ [0,16) → omega"
no_query "$D/ring_buffer_indices.con" "wraparound is kernel-owned → no SMT query"

echo "=== #6 fixed_point_filter differential oracle (sample vectors) ==="
if bash examples/vc_suite/fixed_point_filter_oracle/run_oracle.sh 0 >/dev/null 2>&1; then
  ok "fixed_point_filter oracle agrees with the reference over 50 cases"
else
  no "fixed_point_filter oracle disagreed with the reference"
fi

echo ""
echo "VC-EXAMPLES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
