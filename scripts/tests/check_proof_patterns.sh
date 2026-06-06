#!/usr/bin/env bash
# proof_patterns corpus gate.
#
# examples/proof_patterns/ is the bounded teaching + regression set for source-
# linked proof authoring. This gate pins the load-bearing facts per pattern:
#   - proved patterns kernel-verify (--report check-proofs) and report `proved`;
#   - the non-green states (missing / stale / partial) report honestly;
#   - runtime-safety obligations discharge (omega / bv_decide), and the negative
#     variant stays `unproven`;
#   - `concrete prove --json` carries stable obligation ids;
#   - `--emit-lean` stubs typecheck up to the `sorry` placeholder;
#   - `--workspace` output contains NO proof-registry.json;
#   - the agent-repair fixture's `--check --json` maps a missing proof to
#     `missing_theorem`.
#
# Kernel-dependent checks (check-proofs, emit-lean typecheck, prove --check) are
# guarded by `command -v lake` so a non-nix local run still passes the rest.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PP="examples/proof_patterns"
PASS=0; FAIL=0

ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# assert_contains <label> <needle> <cmd...>
assert_contains(){ local l="$1" n="$2"; shift 2; local o; o="$("$@" 2>&1)"
  if printf '%s' "$o" | grep -qF -- "$n"; then ok "$l"; else no "$l — missing '$n'"; printf '%s\n' "$o"|sed 's/^/      /'|head -8; fi; }
# assert_absent <label> <needle> <cmd...>
assert_absent(){ local l="$1" n="$2"; shift 2; local o; o="$("$@" 2>&1)"
  if printf '%s' "$o" | grep -qF -- "$n"; then no "$l — unexpected '$n'"; else ok "$l"; fi; }
# assert_json <label> <pyexpr> <cmd...>
assert_json(){ local l="$1" e="$2"; shift 2; local o; o="$("$@" 2>/dev/null)"
  if printf '%s' "$o" | python3 -c "import json,sys; d=json.load(sys.stdin); sys.exit(0 if ($e) else 1)" 2>/dev/null; then ok "$l"; else no "$l — JSON/assert failed: $e"; fi; }

echo "=== straight_line (proved refinement) ==="
SL="$PP/straight_line/src/main.con"
assert_contains "straight_line proved" "proof matches current body" "$COMPILER" "$SL" --report proof-status
assert_json "straight_line --json proved + stable id" \
  'd["status"]=="proved" and all("#" in o["id"] for o in d["obligations"])' \
  "$COMPILER" prove "$SL" straight_line.add_three --json

echo "=== array_update (read/write frame) ==="
AU="$PP/array_update/src/main.con"
assert_contains "array_update proved" "proof matches current body" "$COMPILER" "$AU" --report proof-status
assert_json "array_update --json proved + stable id" \
  'd["status"]=="proved" and all("#" in o["id"] for o in d["obligations"])' \
  "$COMPILER" prove "$AU" arr.put --json

echo "=== loop_copy (counted copy loop) ==="
LC="$PP/loop_copy/src/main.con"
assert_contains "loop_copy proved" "proof matches current body" "$COMPILER" "$LC" --report proof-status
assert_json "loop_copy --json proved + stable id" \
  'd["status"]=="proved" and all("#" in o["id"] for o in d["obligations"])' \
  "$COMPILER" prove "$LC" loopcopy.copy2 --json

echo "=== runtime_safety (compiler-discharged + negative) ==="
RT="$PP/runtime_safety/src/main.con"
assert_contains "rt bounds omega-proved" "proved_by_kernel_decision (omega)" "$COMPILER" "$RT" --report contracts
assert_contains "rt overflow bv-proved"  "proved_by_kernel_decision (bv_decide)" "$COMPILER" "$RT" --report contracts
assert_contains "rt negative unproven"   "unproven" "$COMPILER" "$RT" --report contracts

echo "=== stale_missing_partial (honest non-green states) ==="
SMP="$PP/stale_missing_partial/src/main.con"
assert_contains "missing state"  "no registered proof" "$COMPILER" "$SMP" --report proof-status
assert_contains "stale state"    "the body changed"    "$COMPILER" "$SMP" --report proof-status
assert_contains "partial state"  "proved [one_direction]" "$COMPILER" "$SMP" --report proof-status

echo "=== workspace fixture (disposable; no proof-registry.json) ==="
WS="$(mktemp -d)/ws"
assert_contains "workspace generated" "wrote proof workspace" \
  "$COMPILER" prove "$PP/workspace/src/main.con" workspace.scale_by_two --workspace "$WS"
for f in manifest.json context.json link.con.txt check.sh replay.sh README.md Scale_by_twoProofs.lean; do
  [ -f "$WS/$f" ] && ok "workspace file $f" || no "workspace file $f missing"
done
if find "$WS" -name 'proof-registry.json' | grep -q .; then no "workspace has proof-registry.json"; else ok "workspace has no proof-registry.json"; fi
rm -rf "$(dirname "$WS")"

# ---- kernel-dependent checks ----
if command -v lake >/dev/null 2>&1; then
  echo "=== kernel: check-proofs for proved patterns ==="
  assert_contains "straight_line kernel-verified" "1 verified, 0 failed" "$COMPILER" "$SL" --report check-proofs
  assert_contains "array_update kernel-verified"  "1 verified, 0 failed" "$COMPILER" "$AU" --report check-proofs
  assert_contains "loop_copy kernel-verified"    "1 verified, 0 failed" "$COMPILER" "$LC" --report check-proofs
  assert_contains "workspace fn kernel-verified"  "1 verified, 0 failed" "$COMPILER" "$PP/workspace/src/main.con" --report check-proofs
  echo "=== kernel: emit-lean stub typechecks (up to sorry) ==="
  STUB="$(mktemp -d)/Patterns/SL.lean"; mkdir -p "$(dirname "$STUB")"
  "$COMPILER" prove "$SL" straight_line.add_three --emit-lean --out "$STUB" --force >/dev/null 2>&1
  if lake env lean "$STUB" >/dev/null 2>&1; then ok "emit-lean stub typechecks"; else no "emit-lean stub failed to typecheck"; fi
  rm -rf "$(dirname "$(dirname "$STUB")")"
  echo "=== kernel: agent repair fixture maps failure to obligation id ==="
  assert_json "repair --check → missing_theorem on obligation" \
    'd["all_checked"] is False and d["checks"][0]["status"]=="missing_theorem" and "#" in d["checks"][0]["obligation_id"]' \
    "$COMPILER" prove "$PP/repair/src/main.con" repair.needs_proof --check --json
else
  echo "  skip kernel-dependent checks (lake not on PATH)"
fi

echo ""
echo "PROOF-PATTERNS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
