#!/usr/bin/env bash
# Phase 6C #7: cache-free incremental shadow manifest + edit corpus.
#
# A SHADOW of what an incremental cache WOULD do — without ever reusing a compiled
# artifact. For each query it records a reuse-key (semantic input digest + the
# dependency digests it claims to depend on) and an output digest, then, across an
# edit corpus, predicts would_reuse|would_invalidate and ALWAYS recomputes the real
# output. The hard invariant: a query predicted `would_reuse` must have an unchanged
# recomputed output. A FALSE REUSE (predicted reuse, output actually changed) means
# the dependency model is missing an edge — a hard failure.
#
# V1 granularity (may conservatively over-invalidate, per the roadmap): the query is
# the program's structural core; the reuse-key is the set of per-function body
# fingerprints (`--report fingerprints`, position-free); the output digest is
# `--emit-core` (also position-free, so a comment-only edit is a clean no-op on BOTH
# sides rather than a spurious position drift). The final section injects a missing
# dependency edge and proves the shadow catches the resulting false reuse.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
h(){ if command -v sha256sum >/dev/null 2>&1; then sha256sum|cut -d' ' -f1; else shasum -a 256|cut -d' ' -f1; fi; }

# per-function fingerprint lines (position-free), sorted → canonical reuse-key input
fps(){ "$COMPILER" "$1" --report fingerprints 2>/dev/null | grep -E '^\s+[A-Za-z0-9_.]+: ' | sort; }
reuse_key(){ fps "$1" | h; }
out_digest(){ "$COMPILER" "$1" --emit-core 2>/dev/null | h; }

BASE="$TMP/base.con"
cat > "$BASE" <<'EOF'
mod m {
  fn helper(x: i32) -> i32 { return x + 1; }
  fn caller(y: i32) -> i32 { return helper(y) + helper(y); }
  fn main() -> Int { return caller(3) as Int; }
}
EOF
K0=$(reuse_key "$BASE"); O0=$(out_digest "$BASE")
echo "=== baseline manifest ==="
echo "  reuse-key=$K0"
echo "  output   =$O0"
fps "$BASE" | sed 's/^/    query: /'

# shadow_step <label> <edited.con> <expect: reuse|invalidate>
# Predicts from the reuse-key, recomputes the output, and enforces the no-false-reuse
# invariant. Over-invalidation (predicted invalidate, output actually same) is allowed.
shadow_step(){
  local label="$1" f="$2" expect="$3"
  local k o verdict
  k=$(reuse_key "$f"); o=$(out_digest "$f")
  if [ "$k" = "$K0" ]; then verdict=reuse; else verdict=invalidate; fi
  # hard invariant: predicted reuse ⇒ output identical
  if [ "$verdict" = reuse ] && [ "$o" != "$O0" ]; then
    no "$label: FALSE REUSE — key unchanged but output changed (missing dependency edge)"; return
  fi
  if [ "$verdict" = "$expect" ]; then
    ok "$label: predicted $verdict (sound)"
  elif [ "$expect" = reuse ] && [ "$verdict" = invalidate ]; then
    ok "$label: predicted invalidate (conservative over-invalidation; still sound)"
  else
    no "$label: predicted $verdict, expected $expect"
  fi
}

echo "=== edit corpus (predict + recompute + no-false-reuse) ==="
# no-op: byte-identical copy → reuse, output identical
cp "$BASE" "$TMP/noop.con"; shadow_step "no-op" "$TMP/noop.con" reuse
# comment/span-only: fingerprints are position-free → reuse; core is position-free → identical
{ echo "mod m {"; echo "  // a comment"; tail -n +2 "$BASE"; } > "$TMP/comment.con"; shadow_step "comment-only" "$TMP/comment.con" reuse
# private-body edit: helper body changes → fingerprint changes → invalidate
sed 's/return x + 1;/return x + 2;/' "$BASE" > "$TMP/body.con"; shadow_step "private-body" "$TMP/body.con" invalidate
# public signature/type edit: helper param type i32 → i64 → fingerprints change → invalidate
sed 's/fn helper(x: i32) -> i32/fn helper(x: i64) -> i64/; s/return x + 1;/return (x + 1) as i64;/' "$BASE" > "$TMP/sig.con"; shadow_step "signature-change" "$TMP/sig.con" invalidate
# add function: new fingerprint appears → invalidate
sed 's/  fn main/  fn extra() -> i32 { return 9; }\n  fn main/' "$BASE" > "$TMP/add.con"; shadow_step "add-function" "$TMP/add.con" invalidate
# error-then-repair: introduce a type error then repair to identical → reuse (repaired == base)
sed 's/return x + 1;/return true;/' "$BASE" > "$TMP/err.con"   # (broken; not compared for output)
cp "$BASE" "$TMP/repair.con"; shadow_step "error-then-repair (repaired==base)" "$TMP/repair.con" reuse

echo "=== false-hit injection: a reuse-key MISSING a dependency edge must be caught ==="
# A buggy reuse-key that omits `helper` from the keyed set. Editing helper's body then
# leaves the buggy key unchanged (predicts reuse) while the real output changes — the
# shadow's recompute-and-compare must flag this as a false reuse.
buggy_key(){ fps "$1" | grep -v '\.helper:' | h; }   # <-- deliberately drops the helper edge
BK0=$(buggy_key "$BASE")
sed 's/return x + 1;/return x + 5;/' "$BASE" > "$TMP/helper_edit.con"
BK1=$(buggy_key "$TMP/helper_edit.con"); O1=$(out_digest "$TMP/helper_edit.con")
if [ "$BK1" = "$BK0" ] && [ "$O1" != "$O0" ]; then
  ok "false-hit detected: buggy key (helper edge omitted) predicts reuse but output changed → caught"
else
  no "false-hit NOT reproduced (buggy key changed=$([ "$BK1" != "$BK0" ] && echo yes || echo no), output changed=$([ "$O1" != "$O0" ] && echo yes || echo no))"
fi
# ...and the CORRECT key (helper included) correctly invalidates the same edit.
KH=$(reuse_key "$TMP/helper_edit.con")
[ "$KH" != "$K0" ] && ok "correct key invalidates the helper edit (edge present)" \
  || no "correct key missed the helper edit"

echo
echo "check_incremental_shadow: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
