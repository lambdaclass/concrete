#!/usr/bin/env bash
# `concrete prove` v1.1 CLI gate.
#
# Exercises the three read-only sub-modes on their fixtures and asserts a stable
# substring of each output:
#   --emit-link        on constant_time_tag.ct_compare  (source-linked)
#   --show-obligation  on loop_invariant.count_up        (omega leaf O4)
#   --replay           on loop_invariant.count_up        (omega) + a bv_decide call site
#
# Substrings, not byte-exact snapshots: the output is prose-ish and may gain
# fields; these assertions pin the load-bearing facts.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

PASS=0
FAIL=0

# assert <label> <substring> <command...>
assert_contains() {
  local label="$1"; local needle="$2"; shift 2
  local out; out="$("$@" 2>&1)"
  if printf '%s' "$out" | grep -qF -- "$needle"; then
    echo "  ok   $label — found '$needle'"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $label — missing '$needle'"
    printf '%s\n' "$out" | sed 's/^/      /' | head -20
    FAIL=$((FAIL + 1))
  fi
}

# assert_absent <label> <substring> <command...>
assert_absent() {
  local label="$1"; local needle="$2"; shift 2
  local out; out="$("$@" 2>&1)"
  if printf '%s' "$out" | grep -qF -- "$needle"; then
    echo "  FAIL $label — unexpected '$needle'"
    FAIL=$((FAIL + 1))
  else
    echo "  ok   $label — no '$needle'"
    PASS=$((PASS + 1))
  fi
}

CT="examples/constant_time_tag/src/main.con"
LI="examples/loop_invariant/src/main.con"
CC="tests/programs/contract_callsite/main.con"

echo "=== prove --emit-link ==="
assert_contains "emit-link spec"     "#[spec(Concrete.Proof.ctCompareExpr)]" \
  "$COMPILER" prove "$CT" constant_time_tag.ct_compare --emit-link
assert_contains "emit-link proof_by" "#[proof_by(Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct)]" \
  "$COMPILER" prove "$CT" constant_time_tag.ct_compare --emit-link

echo "=== prove --show-obligation ==="
assert_contains "show-obl O4 concl"  "0 ≤ (8 - i)" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --show-obligation O4
assert_contains "show-obl O4 status" "proved_by_kernel_decision (omega)" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --show-obligation O4
assert_contains "show-obl O2 shape"  "loop_preserves" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --show-obligation O2

echo "=== prove --replay ==="
assert_contains "replay omega closes" "still closes" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --replay
assert_absent   "replay no failures"  "FAIL" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --replay
assert_contains "replay bv_decide"    "still closes" \
  "$COMPILER" prove "$CC" demo.letgood --replay

# assert_json <label> <pyexpr-on-d> <command...>  — command must emit valid JSON
# and the python expression (with the parsed dict bound to `d`) must be truthy.
assert_json() {
  local label="$1"; local expr="$2"; shift 2
  local out; out="$("$@" 2>/dev/null)"
  if printf '%s' "$out" | python3 -c "import json,sys; d=json.load(sys.stdin); sys.exit(0 if ($expr) else 1)" 2>/dev/null; then
    echo "  ok   $label"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $label — JSON invalid or assertion failed: $expr"
    printf '%s\n' "$out" | sed 's/^/      /' | head -10
    FAIL=$((FAIL + 1))
  fi
}

echo "=== prove --help=agent (discovery) ==="
assert_contains "help=agent workflow"  "WORKFLOW" "$COMPILER" prove --help=agent
assert_contains "help=agent exit codes" "EXIT CODES" "$COMPILER" prove --help=agent

echo "=== prove --capabilities / --schema (JSON discovery) ==="
assert_json "capabilities schema_version" 'd["schema_version"]=="1"' "$COMPILER" prove --capabilities
assert_json "capabilities prove_json"     'd["features"]["prove_json"] is True' "$COMPILER" prove --capabilities
assert_json "schema required fields"      '"next_actions" in d["required"]' "$COMPILER" prove --schema

echo "=== prove --json (structured proof context) ==="
assert_json "json proved status"   'd["status"]=="proved" and d["proof_link"]["origin"]=="source_linked"' \
  "$COMPILER" prove "$CT" constant_time_tag.ct_compare --json
assert_json "json next_actions"    'len(d["next_actions"])>=1 and all("kind" in a and "command" in a for a in d["next_actions"])' \
  "$COMPILER" prove "$CT" constant_time_tag.ct_compare --json
assert_json "json loop obligations" 'any(o["kind"]=="invariant_init" for o in d["obligations"])' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --json

# assert_exit <label> <expected-code> <command...>
assert_exit() {
  local label="$1"; local want="$2"; shift 2
  "$@" >/dev/null 2>&1; local got=$?
  if [ "$got" -eq "$want" ]; then
    echo "  ok   $label — exit $got"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $label — exit $got, want $want"
    FAIL=$((FAIL + 1))
  fi
}

echo "=== prove exit-code taxonomy (0 proved / 2 missing / 3 stale) ==="
assert_exit "exit proved=0"  0 "$COMPILER" prove "$CT" constant_time_tag.ct_compare --json
assert_exit "exit missing=2" 2 "$COMPILER" prove examples/proof_pressure/src/main.con main.clamp_value --json
assert_exit "exit stale=3"   3 "$COMPILER" prove examples/proof_pressure/src/main.con main.compute_checksum --json

echo "=== prove --json obligation detail (id matches contracts/replay key, spans, hyps) ==="
assert_json "json obligation has span + hyps + stable id" \
  'all(("source_line" in o and "hypotheses" in o and "conclusion" in o and "#" in o["id"]) for o in d["obligations"])' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --json

echo "=== prove JSON modes for subcommands (--show-obligation/--emit-link/--replay --json) ==="
assert_json "show-obligation --json (stable id, hyps, concl)" \
  'd["kind"]=="variant_nonnegative" and "#" in d["id"] and len(d["hypotheses"])>=1 and "conclusion" in d' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --show-obligation 'loop_invariant.count_up@12#O4' --json
assert_json "show-obligation --json accepts short id too" \
  '"O4" in d["id"]' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --show-obligation O4 --json
assert_json "emit-link --json (fingerprint + link_block + next_actions)" \
  'len(d["proof_fingerprint"])>0 and "#[proof_by" in d["link_block"] and any(a["kind"]=="check_proofs" for a in d["next_actions"])' \
  "$COMPILER" prove "$CT" constant_time_tag.ct_compare --emit-link --json
assert_json "replay --json (all_pass + ids match contracts key)" \
  'd["all_pass"] is True and all("#" in o["id"] for o in d["obligations"])' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --replay --json

echo "=== prove --nearest-lemmas (proof-recipe hints) ==="
assert_json "nearest-lemmas recipes + feature lemmas" \
  'len(d["recipes"])>=1 and any(r["kind"]=="invariant_init" and r["tactic"]=="omega" for r in d["recipes"]) and "Concrete.ProofKit.Loops" in d["feature_lemmas"]' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --nearest-lemmas --json
assert_json "capabilities nearest_lemmas=true" 'd["features"]["nearest_lemmas"] is True' "$COMPILER" prove --capabilities
# Scoped to one obligation id (short form) → exactly that recipe.
assert_json "nearest-lemmas scoped to O4" \
  'len(d["recipes"])==1 and d["recipes"][0]["kind"]=="variant_nonnegative" and d["recipes"][0]["tactic"]=="omega"' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --nearest-lemmas O4 --json
# Scoped by the stable id (same key as --json / workspace).
assert_json "nearest-lemmas scoped to stable id" \
  'len(d["recipes"])==1 and "#O2" in d["recipes"][0]["id"]' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --nearest-lemmas 'loop_invariant.count_up@12#O2' --json
assert_json "nearest-lemmas unknown id → error" \
  '"error" in d' \
  "$COMPILER" prove "$LI" loop_invariant.count_up --nearest-lemmas O9 --json

echo "=== prove --emit-lean (compilable single-function Lean stub) ==="
assert_contains "emit-lean namespace"  "namespace Concrete.Proof.Generated.count_up" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --emit-lean
assert_contains "emit-lean theorem"    "theorem count_up_refines_spec" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --emit-lean
assert_contains "emit-lean ends sorry" "= sorry := by" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --emit-lean
assert_contains "emit-lean obl block"  "@12#O1] invariant_init" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --emit-lean
# --out writes; refuses to clobber without --force.
EMIT_OUT="$(mktemp -u)/stub.lean"
assert_contains "emit-lean --out writes" "wrote Lean proof stub" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --emit-lean --out "$EMIT_OUT"
mkdir -p "$(dirname "$EMIT_OUT")"; : > "$EMIT_OUT"   # now it exists
assert_exit "emit-lean clobber refused=1" 1 \
  "$COMPILER" prove "$LI" loop_invariant.count_up --emit-lean --out "$EMIT_OUT"
assert_exit "emit-lean --force=0" 0 \
  "$COMPILER" prove "$LI" loop_invariant.count_up --emit-lean --out "$EMIT_OUT" --force
rm -rf "$(dirname "$EMIT_OUT")"
assert_json "capabilities emit_lean=true" 'd["features"]["emit_lean"] is True' "$COMPILER" prove --capabilities

echo "=== prove --emit-artifacts (failed-obligation bundles) ==="
PP="examples/proof_pressure/src/main.con"
# A cleanly-proved function emits nothing.
assert_contains "artifacts clean → none" "no failed obligations" \
  "$COMPILER" prove "$LI" loop_invariant.count_up --emit-artifacts --out-dir "$(mktemp -d)"
# A missing-proof function emits one function-level bundle with all four files.
ART_DIR="$(mktemp -d)"
assert_contains "artifacts missing → bundle" "failed-obligation artifact" \
  "$COMPILER" prove "$PP" main.clamp_value --emit-artifacts --out-dir "$ART_DIR"
BUNDLE="$ART_DIR/main_clamp_value/main_clamp_value_refines_spec"
for f in context.json failed.lean command.txt README.txt; do
  if [ -f "$BUNDLE/$f" ]; then echo "  ok   artifacts file $f present"; PASS=$((PASS+1));
  else echo "  FAIL artifacts file $f missing"; FAIL=$((FAIL+1)); fi
done
assert_json "artifacts context.json (stable id + recipe)" \
  'd["id"]=="main.clamp_value#refines_spec" and "tactic" in d["recipe"]' \
  cat "$BUNDLE/context.json"
rm -rf "$ART_DIR"
assert_json "capabilities failed_artifacts=true" 'd["features"]["failed_artifacts"] is True' "$COMPILER" prove --capabilities

echo "=== prove --check (structured proof-check JSON) ==="
# Missing-theorem path needs no Lean (returns before invoking the kernel).
assert_json "check missing_theorem (no link)" \
  'd["all_checked"] is False and d["checks"][0]["status"]=="missing_theorem" and d["checks"][0]["theorem"] is None and "lean_error" in d' \
  "$COMPILER" prove "$PP" main.clamp_value --check --json
assert_exit "check missing → exit 2" 2 "$COMPILER" prove "$PP" main.clamp_value --check --json
assert_json "capabilities check=true" 'd["features"]["check"] is True' "$COMPILER" prove --capabilities
# Kernel-backed "checked" path: only when the Lean toolchain is on PATH (always in CI).
if command -v lake >/dev/null 2>&1; then
  assert_json "check proved → checked (kernel)" \
    'd["all_checked"] is True and d["checks"][0]["status"]=="checked" and "." in d["checks"][0]["theorem"]' \
    "$COMPILER" prove "$LI" loop_invariant.count_up --check --json
  assert_exit "check proved → exit 0" 0 "$COMPILER" prove "$LI" loop_invariant.count_up --check --json
else
  echo "  skip kernel --check assertions (lake not on PATH)"
fi

echo "=== prove --workspace (composed proof workspace) ==="
WS="$(mktemp -d)/ws"
WS_OUT="$("$COMPILER" prove "$LI" loop_invariant.count_up --workspace "$WS" 2>&1)"
if printf '%s' "$WS_OUT" | grep -qF "wrote proof workspace"; then
  echo "  ok   workspace generated"; PASS=$((PASS+1))
else
  echo "  FAIL workspace generation — $WS_OUT"; FAIL=$((FAIL+1))
fi
# Expected files present.
for f in manifest.json context.json link.con.txt check.sh replay.sh README.md Count_upProofs.lean; do
  if [ -f "$WS/$f" ]; then echo "  ok   workspace file $f"; PASS=$((PASS+1));
  else echo "  FAIL workspace file $f missing"; FAIL=$((FAIL+1)); fi
done
# At least one obligation JSON file.
if ls "$WS"/obligations/*.json >/dev/null 2>&1; then echo "  ok   workspace obligations/ populated"; PASS=$((PASS+1));
else echo "  FAIL workspace obligations/ empty"; FAIL=$((FAIL+1)); fi
# JSON validity + load-bearing fields.
assert_json "workspace manifest.json (function + status)" \
  'd["function"]=="loop_invariant.count_up" and "status" in d' cat "$WS/manifest.json"
assert_json "workspace context.json (imports + suggested theorem)" \
  '"Concrete.ProofKit.Loops" in d["proofkit_imports"] and any("refines_spec" in t for t in d["suggested_theorems"])' \
  cat "$WS/context.json"
OBL_FILE="$(ls "$WS"/obligations/*.json | head -1)"
assert_json "workspace obligation file (recipe + commands)" \
  '"recipe" in d and "tactic" in d["recipe"] and "replay_command" in d and "check_command" in d and "#" in d["id"]' \
  cat "$OBL_FILE"
assert_contains "workspace link.con.txt block" "#[proof_by(" cat "$WS/link.con.txt"
assert_contains "workspace stub theorem"       "theorem count_up_refines_spec" cat "$WS/Count_upProofs.lean"
# Workspace is a build output, NOT a proof registry: no proof-registry.json anywhere.
if find "$WS" -name 'proof-registry.json' | grep -q .; then
  echo "  FAIL workspace contains a proof-registry.json (must not)"; FAIL=$((FAIL+1))
else
  echo "  ok   workspace has no proof-registry.json"; PASS=$((PASS+1))
fi
assert_json "capabilities workspace=true" 'd["features"]["workspace"] is True' "$COMPILER" prove --capabilities
rm -rf "$(dirname "$WS")"

echo "=== report check-proofs --json (whole-file kernel summary) ==="
if command -v lake >/dev/null 2>&1; then
  assert_json "check-proofs --json (verified, source_linked)" \
    'd["all_checked"] is True and d["schema_version"]=="1" and any(c["status"]=="checked" and c["origin"]=="source_linked" and "source_line" in c for c in d["checks"])' \
    "$COMPILER" "$LI" --report check-proofs --json
  assert_exit "check-proofs --json proved → exit 0" 0 "$COMPILER" "$LI" --report check-proofs --json
else
  echo "  skip check-proofs --json assertions (lake not on PATH)"
fi

echo ""
echo "PROVE-CLI: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
