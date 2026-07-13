#!/usr/bin/env bash
# Phase 6C #6: round-trip / replay artifacts for pass outputs.
#
# For a selected program, record a per-stage SUMMARY HASH + the replay command
# that reproduces that stage's output. V1 hashes the compiler's existing
# deterministic stage renderings (--emit-core, --emit-ssa, --report fingerprints,
# --report obligations) — no new compiler surface, so nothing to drift.
#
# The contract this gate pins:
#   * determinism   — same source/compiler/target => identical pass hashes across runs
#   * comment/span invariance — a comment-only edit leaves the structural stage
#                     hashes unchanged (comments are not semantic input)
#   * structural sensitivity  — a real body edit CHANGES the affected stage hash
#     (so the hash is load-bearing, not a constant)
# and emits a manifest (stage -> hash -> replay command) as the replay artifact.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
h(){ if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1; else shasum -a 256 | cut -d' ' -f1; fi; }
# hash of a stage's output for a program: stage_hash <file> <args...>
stage_hash(){ local f="$1"; shift; "$COMPILER" "$f" "$@" 2>/dev/null | h; }

# The selected program.
cat > "$TMP/p.con" <<'EOF'
mod m {
  fn add1(x: i32) -> i32 { return x + 1; }
  fn dbl(x: i32) -> i32 { return x + x; }
  fn main() -> Int { let a: i32 = add1(2); let b: i32 = dbl(a); return b as Int; }
}
EOF

STAGE_NAME=(core         ssa         fingerprints              obligations)
STAGE_ARGS=("--emit-core" "--emit-ssa" "--report fingerprints" "--report obligations")

echo "=== pass-output manifest (stage -> hash -> replay) ==="
declare -a H0
MANIFEST="$TMP/manifest.txt"
: > "$MANIFEST"
for i in "${!STAGE_NAME[@]}"; do
  # shellcheck disable=SC2086
  H0[$i]=$(stage_hash "$TMP/p.con" ${STAGE_ARGS[$i]})
  if [ -n "${H0[$i]}" ]; then
    printf '%-14s %s   replay: concrete <file> %s\n' "${STAGE_NAME[$i]}" "${H0[$i]}" "${STAGE_ARGS[$i]}" | tee -a "$MANIFEST"
    ok "stage ${STAGE_NAME[$i]}: hash recorded"
  else
    no "stage ${STAGE_NAME[$i]}: empty output (no hash)"
  fi
done

echo "=== determinism: a second run reproduces every hash ==="
det=1
for i in "${!STAGE_NAME[@]}"; do
  # shellcheck disable=SC2086
  h1=$(stage_hash "$TMP/p.con" ${STAGE_ARGS[$i]})
  [ "$h1" = "${H0[$i]}" ] || { det=0; echo "    drift on ${STAGE_NAME[$i]}: ${H0[$i]} != $h1"; }
done
[ "$det" -eq 1 ] && ok "all stage hashes stable across runs" || no "a stage hash was non-deterministic"

echo "=== comment/span invariance: structural (position-free) stages ignore a comment ==="
# A comment-only edit adds a line. The POSITION-FREE stages (core, fingerprints)
# capture structure only and must be unchanged. The POSITION-BEARING stages (ssa,
# obligations) embed source line numbers by design (SSA debug positions; obligation
# source locations) — source position is the axis explicitly EXCLUDED from the
# structural-stability claim — so they shift, and we assert that to pin the
# distinction rather than hide it.
{ echo "mod m {"; echo "  // a comment that carries no semantics"; tail -n +2 "$TMP/p.con"; } > "$TMP/p_comment.con"
core_c=$(stage_hash "$TMP/p_comment.con" --emit-core)
fp_c=$(stage_hash "$TMP/p_comment.con" --report fingerprints)
if [ "$core_c" = "${H0[0]}" ] && [ "$fp_c" = "${H0[2]}" ]; then
  ok "comment-only edit: position-free stages (core/fingerprints) unchanged"
else
  no "comment-only edit changed a position-free stage (core:$core_c fp:$fp_c)"
fi
ssa_c=$(stage_hash "$TMP/p_comment.con" --emit-ssa)
ob_c=$(stage_hash "$TMP/p_comment.con" --report obligations)
if [ "$ssa_c" != "${H0[1]}" ] && [ "$ob_c" != "${H0[3]}" ]; then
  ok "comment-only edit: position-bearing stages (ssa/obligations) shift, as documented"
else
  no "a position-bearing stage unexpectedly ignored a line shift (ssa:$ssa_c ob:$ob_c)"
fi

echo "=== structural sensitivity: a real body edit CHANGES the core hash ==="
# add a statement to main -> core structure differs.
cat > "$TMP/p_struct.con" <<'EOF'
mod m {
  fn add1(x: i32) -> i32 { return x + 1; }
  fn dbl(x: i32) -> i32 { return x + x; }
  fn main() -> Int { let a: i32 = add1(2); let b: i32 = dbl(a); let c: i32 = add1(b); return c as Int; }
}
EOF
core_s=$(stage_hash "$TMP/p_struct.con" --emit-core)
if [ -n "$core_s" ] && [ "$core_s" != "${H0[0]}" ]; then
  ok "structural edit: core hash changed (load-bearing)"
else
  no "structural edit did NOT change the core hash (hash is not load-bearing)"
fi

echo
echo "check_pass_hashes: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
